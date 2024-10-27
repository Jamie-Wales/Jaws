import { Card, CardHeader, CardTitle } from "./ui/card"

export function WelcomeHeader() {
    return (
        <Card className="border-zinc-700 bg-zinc-900">
            <CardHeader className="flex items-center justify-between">
                <CardTitle className="text-zinc-200">HelloWorld</CardTitle>
            </CardHeader>
        </Card>
    )
}
